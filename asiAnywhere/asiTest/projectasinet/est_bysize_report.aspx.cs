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

public partial class Est_Bysize : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "est_bysize_report.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'est_bysize_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BegCustTextBox.Text     = dr["field1"].ToString();
                    EndCustTextBox.Text     = dr["field2"].ToString();
                    BegStyleTextBox.Text    = dr["field3"].ToString();
                    EndStyleTextBox.Text    = dr["field4"].ToString();
                    BegFluteTextBox.Text    = dr["field5"].ToString();
                    EndFluteTextBox.Text    = dr["field6"].ToString();
                    BegTestTextBox.Text     = dr["field7"].ToString();
                    EndTestTextBox.Text     = dr["field8"].ToString();
                    BegDieTextBox.Text      = dr["field9"].ToString();
                    EndDieTextBox.Text      = dr["field10"].ToString();

                    if (dr["chk_field1"].ToString() == "Yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "Yes")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                    if (dr["rd_field1"].ToString() == "Length")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Part#")
                        RadioButtonList1.SelectedIndex = 1;
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
                                
                if (EndStyleTextBox.Text == "")
                    EndStyleTextBox.Text = "zzzz";
                if (EndFluteTextBox.Text == "")
                    EndFluteTextBox.Text = "zzz";
                if (EndTestTextBox.Text == "")
                    EndTestTextBox.Text = "zzzzzzzz";
                if (EndDieTextBox.Text == "")
                    EndDieTextBox.Text = "zzzzzzzzzzzzzzz";

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


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "EstBySizeRep";
        ObjectDataSource1.SelectParameters["prmBegCustomer"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCustomer"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginStyle"].DefaultValue = BegStyleTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndStyle"].DefaultValue = EndStyleTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegFlute"].DefaultValue = BegFluteTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndFlute"].DefaultValue = EndFluteTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegTest"].DefaultValue = BegTestTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndTest"].DefaultValue = EndTestTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegDie"].DefaultValue = BegDieTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDie"].DefaultValue = EndDieTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSortBy"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmFold"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmCorr"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'est_bysize_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, chk_field1, chk_field2, rd_field1) values ('" + UserLogin.UserName + "','est_bysize_report.aspx' , '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BegStyleTextBox.Text.Trim() + "', '" + EndStyleTextBox.Text.Trim() + "', '" + BegFluteTextBox.Text.Trim() + "', '" + EndFluteTextBox.Text.Trim() + "', '" + BegTestTextBox.Text.Trim() + "', '" + EndTestTextBox.Text.Trim() + "', '" + BegDieTextBox.Text.Trim() + "', '" + EndDieTextBox.Text.Trim() + "', '" + HiddenField1.Value + "', '" + HiddenField2.Value + "', '" + RadioButtonList1.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegCustTextBox.Text.Trim() + "', field2 = '" + EndCustTextBox.Text.Trim() + "', field3 = '" + BegStyleTextBox.Text.Trim() + "', field4 = '" + EndStyleTextBox.Text.Trim() + "', field5 = '" + BegFluteTextBox.Text.Trim() + "', field6 = '" + EndFluteTextBox.Text.Trim() + "', field7 = '" + BegTestTextBox.Text.Trim() + "', field8 = '" + EndTestTextBox.Text.Trim() + "', field9 = '" + BegDieTextBox.Text.Trim() + "', field10 = '" + EndDieTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'est_bysize_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vEstSizeFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='est_bysize_report.aspx'</script>");
            }
        }
        catch { }       

    }

   }
