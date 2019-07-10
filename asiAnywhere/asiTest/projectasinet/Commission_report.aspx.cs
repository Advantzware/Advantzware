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

public partial class commission_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "commission_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                Image4.Visible = false;
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
            HyperLink1.Visible = false;
            OutPutFile.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'commission_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox_period.Text = dr["field1"].ToString();
                    TextBox1.Text       = dr["field2"].ToString();
                    TextBox2.Text       = dr["field3"].ToString();
                    TextBox3.Text       = dr["field4"].ToString();
                    TextBox4.Text       = dr["field5"].ToString();
                    TextBox5.Text       = dr["field6"].ToString();
                    TextBox6.Text       = dr["field7"].ToString();
                    TextBox7.Text       = dr["field8"].ToString();
                  

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


                    if (dr["rd_field1"].ToString() == "FG")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Estimated Board")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "Order")
                        RadioButtonList1.SelectedIndex = 2;
                    

                    if (dr["rd_field2"].ToString() == "PTD")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "YTD")
                        RadioButtonList2.SelectedIndex = 1;

                    if (dr["rd_field3"].ToString() == "Customer Part")
                    {
                        RadioButtonList3.SelectedIndex = 0;
                        HiddenField7.Value = "Customer Part";
                    }
                    if (dr["rd_field3"].ToString() == "FG Item")
                    {
                        RadioButtonList3.SelectedIndex = 1;
                        HiddenField7.Value = "FG Item";
                    }
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
                if (TextBox5.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox5.Text = begin.Text;
                    TextBox6.Text = begin.Text;
                }
                if (TextBox_period.Text == "")
                    TextBox_period.Text = "1";
                if (TextBox1.Text == "")
                    TextBox1.Text = "01/01/2009";
                if (TextBox2.Text == "")
                    TextBox2.Text = "12/31/2010";
                if (TextBox4.Text == "")
                    TextBox4.Text = "zzz";
                if (TextBox6.Text == "")
                    TextBox6.Text = "zzzzzzzz";
               
                if (RadioButtonList1.SelectedValue == "")
                    RadioButtonList1.SelectedIndex = 0;
                if (RadioButtonList2.SelectedValue == "")
                    RadioButtonList2.SelectedIndex = 0;
                if (RadioButtonList3.SelectedValue == "")
                    RadioButtonList3.SelectedIndex = 0;

                RadioButtonList_out.SelectedIndex = 0;
            }
            catch { }                   
            
            if (Session["User"] != null)
            {               
                lblUser.Text = UserLogin.UserName;
            }
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

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField4.Value = "FG";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField4.Value = "Estimated Board";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField4.Value = "Order";


        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField5.Value = "PTD";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField5.Value = "YTD";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField6.Value = "Customer Part";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField6.Value = "FG Item";

        /*if (HiddenField6.Value == "")
        {
            HiddenField6.Value = HiddenField7.Value;
        } */ 

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["CommRep"].DefaultValue = "Commission";
        ObjectDataSource1.SelectParameters["prmPtdYtd"].DefaultValue = HiddenField5.Value.Trim();
        ObjectDataSource1.SelectParameters["prmPeriod"].DefaultValue = TextBox_period.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegDate"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegSales"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSales"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegCust"].DefaultValue = TextBox5.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCategory"].DefaultValue = TextBox7.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmDetailed"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["prmPrepCharg"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmCostProfit"].DefaultValue = HiddenField3.Value.Trim();
        ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = HiddenField4.Value.Trim();
        ObjectDataSource1.SelectParameters["prmPrintCustPart"].DefaultValue = HiddenField6.Value.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'commission_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, chk_field1, chk_field2, chk_field3, rd_field1, rd_field2, rd_field3) values ('" + UserLogin.UserName + "','commission_report.aspx' , '" + TextBox_period.Text.Trim() + "', '" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + TextBox7.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox_period.Text.Trim() + "', field2 = '" + TextBox1.Text.Trim() + "', field3 = '" + TextBox2.Text.Trim() + "', field4 = '" + TextBox3.Text.Trim() + "', field5 = '" + TextBox4.Text.Trim() + "', field6 = '" + TextBox5.Text.Trim() + "', field7 = '" + TextBox6.Text.Trim() + "', field8 = '" + TextBox7.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', rd_field1 = '" + HiddenField4.Value + "', rd_field2 = '" + HiddenField5.Value + "', rd_field3= '" + HiddenField6.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'commission_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("commrepfileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;
            

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='commission_report.aspx'</script>");
            }
        }
            catch{}
        
    }
    
   
}
