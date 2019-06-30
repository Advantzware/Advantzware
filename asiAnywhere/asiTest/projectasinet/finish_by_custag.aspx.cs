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

public partial class finish_by_custag : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "finish_by_custag.aspx";
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
                EndCustTextBox.ReadOnly = false;
                Image12.Visible = false;

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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'finish_by_custag.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BeWareTextBox.Text     = dr["field1"].ToString();
                    EndWareTextBox.Text     = dr["field2"].ToString();
                    BegCustTextBox.Text    = dr["field3"].ToString();
                    EndCustTextBox.Text    = dr["field4"].ToString();
                    BeItemTextBox.Text    = dr["field5"].ToString();
                    EndItemTextBox.Text    = dr["field6"].ToString();
                    

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

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
                if (BegCustTextBox.Text == "")
                {
                    Label custn = (Label)FormView2.FindControl("CustLabel");
                    BegCustTextBox.Text = custn.Text;
                    EndCustTextBox.Text = custn.Text;
                }
                                
                if (EndWareTextBox.Text == "")
                    EndWareTextBox.Text = "zzz";
                
                if (EndItemTextBox.Text == "")
                    EndItemTextBox.Text = "zzzzzzzzzzzzzzz";

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
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Finished";
        ObjectDataSource1.SelectParameters["prmBeWare"].DefaultValue = BeWareTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWare"].DefaultValue = EndWareTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = BeItemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = EndItemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustOwned"].DefaultValue = Convert.ToString(CheckBox1.Checked);
               
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'finish_by_custag.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6,  chk_field1) values ('" + UserLogin.UserName + "','finish_by_custag.aspx' , '" + BeWareTextBox.Text.Trim() + "', '" + EndWareTextBox.Text.Trim() + "', '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BeItemTextBox.Text.Trim() + "', '" + EndItemTextBox.Text.Trim() + "', '" + Convert.ToString(CheckBox1.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BeWareTextBox.Text.Trim() + "', field2 = '" + EndWareTextBox.Text.Trim() + "', field3 = '" + BegCustTextBox.Text.Trim() + "', field4 = '" + EndCustTextBox.Text.Trim() + "', field5 = '" + BeItemTextBox.Text.Trim() + "', field6 = '" + EndItemTextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'finish_by_custag.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("ytdcstLabel");
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
