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

public partial class est_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "est_list_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;

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


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'est_list_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BegCustTextBox.Text     = dr["field1"].ToString();
                    EndCustTextBox.Text     = dr["field2"].ToString();
                    BegSalrepTextBox.Text   = dr["field3"].ToString();
                    EndSalrepTextBox.Text   = dr["field4"].ToString();
                    BegEstTextBox.Text      = dr["field5"].ToString();
                    EndEstTextBox.Text      = dr["field6"].ToString();
                    BegAdddateTextBox.Text  = dr["field7"].ToString();
                    EndAdddateTextBox.Text  = dr["field8"].ToString();
                    BegModdateTextBox.Text  = dr["field9"].ToString();
                    EndModdateTextBox.Text  = dr["field10"].ToString();
                    BegMachTextBox.Text     = dr["field11"].ToString();
                    EndMachTextBox.Text     = dr["field12"].ToString();

                    if (dr["chk_field1"].ToString() == "Yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "Yes")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    
                    if (dr["chk_field3"].ToString() == "Yes")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "Yes")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false; 
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
                if (EndCustTextBox.Text == "")
                    EndCustTextBox.Text = "zzzzzzzz";
                if (EndAdddateTextBox.Text == "")
                    EndAdddateTextBox.Text = "12/31/9999";
                if (EndModdateTextBox.Text == "")
                    EndModdateTextBox.Text = "12/31/9999";
                if (EndSalrepTextBox.Text == "")
                    EndSalrepTextBox.Text = "zzz";
                if (EndEstTextBox.Text == "")
                    EndEstTextBox.Text = "zzzzzzzz";
                if (EndMachTextBox.Text == "")
                    EndMachTextBox.Text = "zzzzzzzz";
       
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

        if (CheckBox1.Checked)     
            HiddenField1.Value = "Yes";      
        else        
            HiddenField1.Value = "No";
        if (CheckBox2.Checked)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";
        if (CheckBox3.Checked)
            HiddenField3.Value = "Yes";
        else
            HiddenField3.Value = "No";
        if (CheckBox4.Checked)
            HiddenField4.Value = "Yes";
        else
            HiddenField4.Value = "No";


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "EstListRep";
        ObjectDataSource1.SelectParameters["prmBegCustomer"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCustomer"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegAddDate"].DefaultValue = BegAdddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndAddDate"].DefaultValue = EndAdddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegModDate"].DefaultValue = BegModdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndModDate"].DefaultValue = EndModdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegEst"].DefaultValue = BegEstTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndEst"].DefaultValue = EndEstTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegMach"].DefaultValue = BegMachTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndMach"].DefaultValue = EndMachTextBox.Text.Trim();                                             
        ObjectDataSource1.SelectParameters["prmBegSalrep"].DefaultValue = BegSalrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSalrep"].DefaultValue = EndSalrepTextBox.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmBreak"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmBooked"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmNotBooked"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'est_list_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, chk_field1, chk_field2, chk_field3, chk_field4) values ('" + UserLogin.UserName + "','est_list_report.aspx' , '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BegSalrepTextBox.Text.Trim() + "', '" + EndSalrepTextBox.Text.Trim() + "', '" + BegEstTextBox.Text.Trim() + "', '" + EndEstTextBox.Text.Trim() + "', '" + BegAdddateTextBox.Text.Trim() + "', '" + EndAdddateTextBox.Text.Trim() + "', '" + BegModdateTextBox.Text.Trim() + "', '" + EndModdateTextBox.Text.Trim() + "', '" + BegMachTextBox.Text.Trim() + "', '" + EndMachTextBox.Text.Trim() + "', '" + HiddenField1.Value + "', '" + HiddenField2.Value + "', '" + HiddenField3.Value + "', '" + HiddenField4.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {                
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegCustTextBox.Text.Trim() + "', field2 = '" + EndCustTextBox.Text.Trim() + "', field3 = '" + BegSalrepTextBox.Text.Trim() + "', field4 = '" + EndSalrepTextBox.Text.Trim() + "', field5 = '" + BegEstTextBox.Text.Trim() + "', field6 = '" + EndEstTextBox.Text.Trim() + "', field7 = '" + BegAdddateTextBox.Text.Trim() + "', field8 = '" + EndAdddateTextBox.Text.Trim() + "', field9 = '" + BegModdateTextBox.Text.Trim() + "', field10 = '" + EndModdateTextBox.Text.Trim() + "', field11 = '" + BegMachTextBox.Text.Trim() + "', field12 = '" + EndMachTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'est_list_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='est_list_report.aspx'</script>");
            }
        }
        catch { }               
    }

   }
