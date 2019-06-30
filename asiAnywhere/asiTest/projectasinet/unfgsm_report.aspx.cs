#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class unfgsm_report: System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "unfgsm_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
        
            if (Session["User"] != null)
            {                
                lblUser.Text = UserLogin.UserName;
            }
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'unfgsm_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text       = dr["field1"].ToString();
                    TextBox6.Text       = dr["field2"].ToString();
                    TextBox2.Text       = dr["field3"].ToString();
                    TextBox5.Text       = dr["field4"].ToString();
                    TextBox3.Text       = dr["field5"].ToString();
                    TextBox4.Text       = dr["field6"].ToString();
                    TextBox7.Text       = dr["field7"].ToString();
                    TextBox8.Text       = dr["field8"].ToString();


                    if (dr["rd_field1"].ToString() == "Open")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Closed")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "All")
                        RadioButtonList1.SelectedIndex = 2;                                          
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {
                if (TextBox2.Text == "")
                {
                    Label custn = (Label)FormView2.FindControl("CustLabel");
                    TextBox2.Text = custn.Text;
                    TextBox5.Text = custn.Text;
                }             

                if (TextBox4.Text == "")
                    TextBox4.Text = "zzzzzzzzzzzzzzz";

                if (TextBox8.Text == "")
                    TextBox8.Text = "zzz";

                if (TextBox1.Text == "")
                    TextBox1.Text = "01/01/001";

                if (TextBox6.Text == "")
                    TextBox6.Text = "12/31/9999";

                RadioButtonList_out.SelectedIndex = 0;                               
            }
            catch { }
            
        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UnFgSumm";
        ObjectDataSource1.SelectParameters["prmBegOrdDate"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrdDate"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginItem"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginWhse"].DefaultValue = TextBox7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWhse"].DefaultValue = TextBox8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOrdStatus"].DefaultValue = Convert.ToString(RadioButtonList1.SelectedValue);                             
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'unfgsm_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6,field7, field8, rd_field1) values ('" + UserLogin.UserName + "','unfgsm_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox6.Text.Trim() + "', '" + TextBox2.Text.Trim() + "', '" + TextBox5.Text.Trim() + "', '" + TextBox3.Text.Trim() + "', '" + TextBox4.Text.Trim() + "', '" + TextBox7.Text.Trim() + "', '" + TextBox8.Text.Trim() + "','" + Convert.ToString(RadioButtonList1.SelectedValue) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox6.Text.Trim() + "', field3 = '" + TextBox2.Text.Trim() + "', field4 = '" + TextBox5.Text.Trim() + "', field5 = '" + TextBox3.Text.Trim() + "', field6 = '" + TextBox4.Text.Trim() + "',field7 = '" + TextBox7.Text.Trim() + "',field8 = '" + TextBox8.Text.Trim() + "', rd_field1 = '" + Convert.ToString(RadioButtonList1.SelectedValue) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'unfgsm_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {
            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("vUnfgsmFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";                
            }
        }
        catch { }       
        
    }

   }
