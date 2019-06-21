
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web;
#endregion

public partial class estimate_analysis : System.Web.UI.Page
{
    string puge = "";
    protected void Page_Load(object sender, System.EventArgs e)
    {
       
              
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_print.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {

                    vendordiv.Visible = false;
                    Session["estimate_analysis_vendor_no"] = "";
                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    vendordiv.Visible = false;
                    Session["estimate_analysis_vendor_no"] = "";
                }

            }

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (Request.QueryString["rowcnt"] == "no")
        {
            HiddenField1.Value = "No";
            if (GridView1.Rows.Count > 0)
            {
                vendordiv.Visible = true;
            }
            else
            {
                FormView1.ChangeMode(FormViewMode.Insert);
            }

            pugetable.Visible = false;

            try
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                conn.Open();

                SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
                cmd_delete.ExecuteNonQuery();
                conn.Close();
            }
            catch { }
        }



        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
       
    }
    
    protected void ClearQtyClick(object sender, EventArgs e)
    {
        TextBox Qty1 = (TextBox)FormView1.FindControl("TextBox1");
        TextBox Qty2 = (TextBox)FormView1.FindControl("TextBox2");
        TextBox Qty3 = (TextBox)FormView1.FindControl("TextBox3");
        TextBox Qty4 = (TextBox)FormView1.FindControl("TextBox4");
        TextBox Qty5 = (TextBox)FormView1.FindControl("TextBox5");
        TextBox Qty6 = (TextBox)FormView1.FindControl("TextBox6");
        TextBox Qty7 = (TextBox)FormView1.FindControl("TextBox7");
        TextBox Qty8 = (TextBox)FormView1.FindControl("TextBox8");
        TextBox Qty9 = (TextBox)FormView1.FindControl("TextBox9");
        TextBox Qty10 = (TextBox)FormView1.FindControl("TextBox10");
        TextBox Qty11 = (TextBox)FormView1.FindControl("TextBox11");
        TextBox Qty12 = (TextBox)FormView1.FindControl("TextBox12");
        TextBox Qty13 = (TextBox)FormView1.FindControl("TextBox13");
        TextBox Qty14 = (TextBox)FormView1.FindControl("TextBox14");
        TextBox Qty15 = (TextBox)FormView1.FindControl("TextBox15");
        TextBox Qty16 = (TextBox)FormView1.FindControl("TextBox16");
        TextBox Qty17 = (TextBox)FormView1.FindControl("TextBox17");
        TextBox Qty18 = (TextBox)FormView1.FindControl("TextBox18");
        TextBox Qty19 = (TextBox)FormView1.FindControl("TextBox19");
        TextBox Qty20 = (TextBox)FormView1.FindControl("TextBox20");
        TextBox Qty21 = (TextBox)FormView1.FindControl("TextBox21");
        TextBox Qty22 = (TextBox)FormView1.FindControl("TextBox22");
        TextBox Qty23 = (TextBox)FormView1.FindControl("TextBox23");
        TextBox Qty24 = (TextBox)FormView1.FindControl("TextBox24");
        TextBox Qty25 = (TextBox)FormView1.FindControl("TextBox25");
        TextBox Qty26 = (TextBox)FormView1.FindControl("TextBox26");
        TextBox Qty27 = (TextBox)FormView1.FindControl("TextBox27");
        TextBox Qty28 = (TextBox)FormView1.FindControl("TextBox28");

        TextBox Rel1 = (TextBox)FormView1.FindControl("TextBox29");
        TextBox Rel2 = (TextBox)FormView1.FindControl("TextBox30");
        TextBox Rel3 = (TextBox)FormView1.FindControl("TextBox31");
        TextBox Rel4 = (TextBox)FormView1.FindControl("TextBox32");
        TextBox Rel5 = (TextBox)FormView1.FindControl("TextBox33");
        TextBox Rel6 = (TextBox)FormView1.FindControl("TextBox34");
        TextBox Rel7 = (TextBox)FormView1.FindControl("TextBox35");
        TextBox Rel8 = (TextBox)FormView1.FindControl("TextBox36");
        TextBox Rel9 = (TextBox)FormView1.FindControl("TextBox37");
        TextBox Rel10 = (TextBox)FormView1.FindControl("TextBox38");
        TextBox Rel11 = (TextBox)FormView1.FindControl("TextBox39");
        TextBox Rel12 = (TextBox)FormView1.FindControl("TextBox40");
        TextBox Rel13 = (TextBox)FormView1.FindControl("TextBox41");
        TextBox Rel14 = (TextBox)FormView1.FindControl("TextBox42");
        TextBox Rel15 = (TextBox)FormView1.FindControl("TextBox43");
        TextBox Rel16 = (TextBox)FormView1.FindControl("TextBox44");
        TextBox Rel17 = (TextBox)FormView1.FindControl("TextBox45");
        TextBox Rel18 = (TextBox)FormView1.FindControl("TextBox46");
        TextBox Rel19 = (TextBox)FormView1.FindControl("TextBox47");
        TextBox Rel20 = (TextBox)FormView1.FindControl("TextBox48");
        TextBox Rel21 = (TextBox)FormView1.FindControl("TextBox49");
        TextBox Rel22 = (TextBox)FormView1.FindControl("TextBox50");
        TextBox Rel23 = (TextBox)FormView1.FindControl("TextBox51");
        TextBox Rel24 = (TextBox)FormView1.FindControl("TextBox52");
        TextBox Rel25 = (TextBox)FormView1.FindControl("TextBox53");
        TextBox Rel26 = (TextBox)FormView1.FindControl("TextBox54");
        TextBox Rel27 = (TextBox)FormView1.FindControl("TextBox55");
        TextBox Rel28 = (TextBox)FormView1.FindControl("TextBox56");

        Qty1.Text = "";
        Qty2.Text = "";
        Qty3.Text = "";
        Qty4.Text = "";
        Qty5.Text = "";
        Qty6.Text = "";
        Qty7.Text = "";
        Qty8.Text = "";
        Qty9.Text = "";
        Qty10.Text = "";
        Qty11.Text = "";
        Qty12.Text = "";
        Qty13.Text = "";
        Qty14.Text = "";
        Qty15.Text = "";
        Qty16.Text = "";
        Qty17.Text = "";
        Qty18.Text = "";
        Qty19.Text = "";
        Qty20.Text = "";
        Qty21.Text = "";
        Qty22.Text = "";
        Qty23.Text = "";
        Qty24.Text = "";
        Qty25.Text = "";
        Qty26.Text = "";
        Qty27.Text = "";
        Qty28.Text = "";

        Rel1.Text = "";
        Rel2.Text = "";
        Rel3.Text = "";
        Rel4.Text = "";
        Rel5.Text = "";
        Rel6.Text = "";
        Rel7.Text = "";
        Rel8.Text = "";
        Rel9.Text = "";
        Rel10.Text = "";
        Rel11.Text = "";
        Rel12.Text = "";
        Rel13.Text = "";
        Rel14.Text = "";
        Rel15.Text = "";
        Rel16.Text = "";
        Rel17.Text = "";
        Rel18.Text = "";
        Rel19.Text = "";
        Rel20.Text = "";
        Rel21.Text = "";
        Rel22.Text = "";
        Rel23.Text = "";
        Rel24.Text = "";
        Rel25.Text = "";
        Rel26.Text = "";
        Rel27.Text = "";
        Rel28.Text = "";
    }

    protected void PurgeQtyClick(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        HiddenField1.Value = "Yes";
        if (GridView1.Rows.Count > 0)
        {
            vendordiv.Visible = true;
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Insert);
        }
                     
        pugetable.Visible = false;  
         try
        {
           SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
           conn.Open();
           
          SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
             cmd_delete.ExecuteNonQuery();
             conn.Close();
         }
        catch {}
         

        /*Page.ClientScript.RegisterStartupScript
                            (this.GetType(), "displayQtyScrDiv", "displayQtyScrDiv();", true);*/
    }
    protected void AppendQtyClick(object sender, EventArgs e)
    {
        HiddenField1.Value = "No";
        if (GridView1.Rows.Count > 0)
        {
            vendordiv.Visible = true;
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Insert);
        }

        pugetable.Visible = false;  

        try
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
            cmd_delete.ExecuteNonQuery();
            conn.Close();
        }
        catch { }
        
        //Page.ClientScript.RegisterStartupScript(this.GetType(), "displayQtyScrDiv", "displayQtyScrDiv();", true);
    }
   
    protected void SaveBtnClick(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Qty1 = (TextBox)FormView1.FindControl("TextBox1");
        TextBox Qty2 = (TextBox)FormView1.FindControl("TextBox2");
        TextBox Qty3 = (TextBox)FormView1.FindControl("TextBox3");
        TextBox Qty4 = (TextBox)FormView1.FindControl("TextBox4");
        TextBox Qty5 = (TextBox)FormView1.FindControl("TextBox5");
        TextBox Qty6 = (TextBox)FormView1.FindControl("TextBox6");
        TextBox Qty7 = (TextBox)FormView1.FindControl("TextBox7");
        TextBox Qty8 = (TextBox)FormView1.FindControl("TextBox8");
        TextBox Qty9 = (TextBox)FormView1.FindControl("TextBox9");
        TextBox Qty10 = (TextBox)FormView1.FindControl("TextBox10");
        TextBox Qty11 = (TextBox)FormView1.FindControl("TextBox11");
        TextBox Qty12 = (TextBox)FormView1.FindControl("TextBox12");
        TextBox Qty13 = (TextBox)FormView1.FindControl("TextBox13");
        TextBox Qty14 = (TextBox)FormView1.FindControl("TextBox14");
        TextBox Qty15 = (TextBox)FormView1.FindControl("TextBox15");
        TextBox Qty16 = (TextBox)FormView1.FindControl("TextBox16");
        TextBox Qty17 = (TextBox)FormView1.FindControl("TextBox17");
        TextBox Qty18 = (TextBox)FormView1.FindControl("TextBox18");
        TextBox Qty19 = (TextBox)FormView1.FindControl("TextBox19");
        TextBox Qty20 = (TextBox)FormView1.FindControl("TextBox20");
        TextBox Qty21 = (TextBox)FormView1.FindControl("TextBox21");
        TextBox Qty22 = (TextBox)FormView1.FindControl("TextBox22");
        TextBox Qty23 = (TextBox)FormView1.FindControl("TextBox23");
        TextBox Qty24 = (TextBox)FormView1.FindControl("TextBox24");
        TextBox Qty25 = (TextBox)FormView1.FindControl("TextBox25");
        TextBox Qty26 = (TextBox)FormView1.FindControl("TextBox26");
        TextBox Qty27 = (TextBox)FormView1.FindControl("TextBox27");
        TextBox Qty28 = (TextBox)FormView1.FindControl("TextBox28");

        TextBox Rel1 = (TextBox)FormView1.FindControl("TextBox29");
        TextBox Rel2 = (TextBox)FormView1.FindControl("TextBox30");
        TextBox Rel3 = (TextBox)FormView1.FindControl("TextBox31");
        TextBox Rel4 = (TextBox)FormView1.FindControl("TextBox32");
        TextBox Rel5 = (TextBox)FormView1.FindControl("TextBox33");
        TextBox Rel6 = (TextBox)FormView1.FindControl("TextBox34");
        TextBox Rel7 = (TextBox)FormView1.FindControl("TextBox35");
        TextBox Rel8 = (TextBox)FormView1.FindControl("TextBox36");
        TextBox Rel9 = (TextBox)FormView1.FindControl("TextBox37");
        TextBox Rel10 = (TextBox)FormView1.FindControl("TextBox38");
        TextBox Rel11 = (TextBox)FormView1.FindControl("TextBox39");
        TextBox Rel12 = (TextBox)FormView1.FindControl("TextBox40");
        TextBox Rel13 = (TextBox)FormView1.FindControl("TextBox41");
        TextBox Rel14 = (TextBox)FormView1.FindControl("TextBox42");
        TextBox Rel15 = (TextBox)FormView1.FindControl("TextBox43");
        TextBox Rel16 = (TextBox)FormView1.FindControl("TextBox44");
        TextBox Rel17 = (TextBox)FormView1.FindControl("TextBox45");
        TextBox Rel18 = (TextBox)FormView1.FindControl("TextBox46");
        TextBox Rel19 = (TextBox)FormView1.FindControl("TextBox47");
        TextBox Rel20 = (TextBox)FormView1.FindControl("TextBox48");
        TextBox Rel21 = (TextBox)FormView1.FindControl("TextBox49");
        TextBox Rel22 = (TextBox)FormView1.FindControl("TextBox50");
        TextBox Rel23 = (TextBox)FormView1.FindControl("TextBox51");
        TextBox Rel24 = (TextBox)FormView1.FindControl("TextBox52");
        TextBox Rel25 = (TextBox)FormView1.FindControl("TextBox53");
        TextBox Rel26 = (TextBox)FormView1.FindControl("TextBox54");
        TextBox Rel27 = (TextBox)FormView1.FindControl("TextBox55");
        TextBox Rel28 = (TextBox)FormView1.FindControl("TextBox56");

        if (Qty1.Text == "")
            Qty1.Text = "0";
        if (Qty2.Text == "")
            Qty2.Text = "0";
        if (Qty3.Text == "")
            Qty3.Text = "0";
        if (Qty4.Text == "")
            Qty4.Text = "0";
        if (Qty5.Text == "")
            Qty5.Text = "0";
        if (Qty6.Text == "")
            Qty6.Text = "0";
        if (Qty7.Text == "")
            Qty7.Text = "0";
        if (Qty8.Text == "")
            Qty8.Text = "0";
        if (Qty9.Text == "")
            Qty9.Text = "0";
        if (Qty10.Text == "")
            Qty10.Text = "0";
        if (Qty11.Text == "")
            Qty11.Text = "0";
        if (Qty12.Text == "")
            Qty12.Text = "0";
        if (Qty13.Text == "")
            Qty13.Text = "0";
        if (Qty14.Text == "")
            Qty14.Text = "0";
        if (Qty15.Text == "")
            Qty15.Text = "0";
        if (Qty16.Text == "")
            Qty16.Text = "0";
        if (Qty17.Text == "")
            Qty17.Text = "0";
        if (Qty18.Text == "")
            Qty18.Text = "0";
        if (Qty19.Text == "")
            Qty19.Text = "0";
        if (Qty20.Text == "")
            Qty20.Text = "0";
        if (Qty21.Text == "")
            Qty21.Text = "0";
        if (Qty22.Text == "")
            Qty22.Text = "0";
        if (Qty23.Text == "")
            Qty23.Text = "0";
        if (Qty24.Text == "")
            Qty24.Text = "0";
        if (Qty25.Text == "")
            Qty25.Text = "0";
        if (Qty26.Text == "")
            Qty26.Text = "0";
        if (Qty27.Text == "")
            Qty27.Text = "0";
        if (Qty28.Text == "")
            Qty28.Text = "0";
        if (Rel1.Text == "")
            Rel1.Text = "0";
        if (Rel2.Text == "")
            Rel2.Text = "0";
        if (Rel3.Text == "")
            Rel3.Text = "0";
        if (Rel4.Text == "")
            Rel4.Text = "0";
        if (Rel5.Text == "")
            Rel5.Text = "0";
        if (Rel6.Text == "")
            Rel6.Text = "0";
        if (Rel7.Text == "")
            Rel7.Text = "0";
        if (Rel8.Text == "")
            Rel8.Text = "0";
        if (Rel9.Text == "")
            Rel9.Text = "0";
        if (Rel10.Text == "")
            Rel10.Text = "0";
        if (Rel11.Text == "")
            Rel11.Text = "0";
        if (Rel12.Text == "")
            Rel12.Text = "0";
        if (Rel13.Text == "")
            Rel13.Text = "0";
        if (Rel14.Text == "")
            Rel14.Text = "0";
        if (Rel15.Text == "")
            Rel15.Text = "0";
        if (Rel16.Text == "")
            Rel16.Text = "0";
        if (Rel17.Text == "")
            Rel17.Text = "0";
        if (Rel18.Text == "")
            Rel18.Text = "0";
        if (Rel19.Text == "")
            Rel19.Text = "0";
        if (Rel20.Text == "")
            Rel20.Text = "0";
        if (Rel21.Text == "")
            Rel21.Text = "0";
        if (Rel22.Text == "")
            Rel22.Text = "0";
        if (Rel23.Text == "")
            Rel23.Text = "0";
        if (Rel24.Text == "")
            Rel24.Text = "0";
        if (Rel25.Text == "")
            Rel25.Text = "0";
        if (Rel26.Text == "")
            Rel26.Text = "0";
        if (Rel27.Text == "")
            Rel27.Text = "0";
        if (Rel28.Text == "")
            Rel28.Text = "0";

        CheckBox dogsa = (CheckBox)FormView1.FindControl("CheckBox3");
        CheckBox domr = (CheckBox)FormView1.FindControl("CheckBox2");
        CheckBox dospeed = (CheckBox)FormView1.FindControl("CheckBox1");
        CheckBox droprc = (CheckBox)FormView1.FindControl("CheckBox4");

        string do_gsa = "";
        string do_mr = "";
        string do_speed = "";
        string drop_rc = "";
        if (dogsa.Checked)
            do_gsa = "True";
        else
            do_gsa = "False";
        if (domr.Checked)
            do_mr = "True";
        else
            do_mr = "False";
        if (dospeed.Checked)
            do_speed = "True";
        else
            do_speed = "False";
        if (droprc.Checked)
            drop_rc = "True";
        else
            drop_rc = "False";


        string estimate = Convert.ToString(Session["order_corrugated_est"]);
        Int32 formno = Convert.ToInt32(Session["order_corrugated_formno"]);
        Int32 blankno = Convert.ToInt32(Session["order_corrugated_blankno"]);

        string error = "";
        string rec_id = "";

        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        DataTable dt = new DataTable();
        //ds = corr.SelectQuantity(UserLogin.UserName, "doCalc", Convert.ToInt32(Qty1.Text), Convert.ToInt32(Qty2.Text), Convert.ToInt32(Qty3.Text), Convert.ToInt32(Qty4.Text), Convert.ToInt32(Qty5.Text), Convert.ToInt32(Qty6.Text), Convert.ToInt32(Qty7.Text), Convert.ToInt32(Qty8.Text), Convert.ToInt32(Qty9.Text), Convert.ToInt32(Qty10.Text), Convert.ToInt32(Qty11.Text), Convert.ToInt32(Qty12.Text), Convert.ToInt32(Qty13.Text), Convert.ToInt32(Qty14.Text), Convert.ToInt32(Qty15.Text), Convert.ToInt32(Qty16.Text), Convert.ToInt32(Qty17.Text), Convert.ToInt32(Qty18.Text), Convert.ToInt32(Qty19.Text), Convert.ToInt32(Qty20.Text), Convert.ToInt32(Qty21.Text), Convert.ToInt32(Qty22.Text), Convert.ToInt32(Qty23.Text), Convert.ToInt32(Qty24.Text), Convert.ToInt32(Qty25.Text), Convert.ToInt32(Qty26.Text), Convert.ToInt32(Qty27.Text), Convert.ToInt32(Qty28.Text), Convert.ToInt32(Rel1.Text), Convert.ToInt32(Rel2.Text), Convert.ToInt32(Rel3.Text), Convert.ToInt32(Rel4.Text), Convert.ToInt32(Rel5.Text), Convert.ToInt32(Rel6.Text), Convert.ToInt32(Rel7.Text), Convert.ToInt32(Rel8.Text), Convert.ToInt32(Rel9.Text), Convert.ToInt32(Rel10.Text), Convert.ToInt32(Rel11.Text), Convert.ToInt32(Rel12.Text), Convert.ToInt32(Rel13.Text), Convert.ToInt32(Rel14.Text), Convert.ToInt32(Rel15.Text), Convert.ToInt32(Rel16.Text), Convert.ToInt32(Rel17.Text), Convert.ToInt32(Rel18.Text), Convert.ToInt32(Rel19.Text), Convert.ToInt32(Rel20.Text), Convert.ToInt32(Rel21.Text), Convert.ToInt32(Rel22.Text), Convert.ToInt32(Rel23.Text), Convert.ToInt32(Rel24.Text), Convert.ToInt32(Rel25.Text), Convert.ToInt32(Rel26.Text), Convert.ToInt32(Rel27.Text), Convert.ToInt32(Rel28.Text), 0, Convert.ToString(dogsa.Checked), Convert.ToString(domr.Checked), Convert.ToString(dospeed.Checked), Convert.ToString(droprc.Checked), "", "", estimate, formno, blankno, HiddenField1.Value.Trim(), ref rec_id);
        ds = corr.SelectQuantity(UserLogin.UserName, "doCalc", Convert.ToInt32(Qty1.Text), Convert.ToInt32(Qty2.Text), Convert.ToInt32(Qty3.Text), Convert.ToInt32(Qty4.Text), Convert.ToInt32(Qty5.Text), Convert.ToInt32(Qty6.Text), Convert.ToInt32(Qty7.Text), Convert.ToInt32(Qty8.Text), Convert.ToInt32(Qty9.Text), Convert.ToInt32(Qty10.Text), Convert.ToInt32(Qty11.Text), Convert.ToInt32(Qty12.Text), Convert.ToInt32(Qty13.Text), Convert.ToInt32(Qty14.Text), Convert.ToInt32(Qty15.Text), Convert.ToInt32(Qty16.Text), Convert.ToInt32(Qty17.Text), Convert.ToInt32(Qty18.Text), Convert.ToInt32(Qty19.Text), Convert.ToInt32(Qty20.Text), Convert.ToInt32(Qty21.Text), Convert.ToInt32(Qty22.Text), Convert.ToInt32(Qty23.Text), Convert.ToInt32(Qty24.Text), Convert.ToInt32(Qty25.Text), Convert.ToInt32(Qty26.Text), Convert.ToInt32(Qty27.Text), Convert.ToInt32(Qty28.Text), Convert.ToInt32(Rel1.Text), Convert.ToInt32(Rel2.Text), Convert.ToInt32(Rel3.Text), Convert.ToInt32(Rel4.Text), Convert.ToInt32(Rel5.Text), Convert.ToInt32(Rel6.Text), Convert.ToInt32(Rel7.Text), Convert.ToInt32(Rel8.Text), Convert.ToInt32(Rel9.Text), Convert.ToInt32(Rel10.Text), Convert.ToInt32(Rel11.Text), Convert.ToInt32(Rel12.Text), Convert.ToInt32(Rel13.Text), Convert.ToInt32(Rel14.Text), Convert.ToInt32(Rel15.Text), Convert.ToInt32(Rel16.Text), Convert.ToInt32(Rel17.Text), Convert.ToInt32(Rel18.Text), Convert.ToInt32(Rel19.Text), Convert.ToInt32(Rel20.Text), Convert.ToInt32(Rel21.Text), Convert.ToInt32(Rel22.Text), Convert.ToInt32(Rel23.Text), Convert.ToInt32(Rel24.Text), Convert.ToInt32(Rel25.Text), Convert.ToInt32(Rel26.Text), Convert.ToInt32(Rel27.Text), Convert.ToInt32(Rel28.Text), 0, do_gsa, do_mr, do_speed, drop_rc, "", "", estimate, formno, blankno, HiddenField1.Value.Trim(),HiddenField_Vendor.Value.Trim(), ref rec_id);

        //try
        //{
       
           if (do_gsa == "True")
           {
               Session["estimate_analysis_gsa_mat"] = ds.Tables[0].Rows[0][4].ToString();
               Session["estimate_analysis_gsa_lab"] = ds.Tables[0].Rows[0][5].ToString();
               Session["estimate_analysis_gsa_war"] = ds.Tables[0].Rows[0][6].ToString();
               Session["estimate_analysis_gsa_fm"] = ds.Tables[0].Rows[0][7].ToString();
               Session["estimate_analysis_gsa_month"] = ds.Tables[0].Rows[0][9].ToString();
               SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
               conn.Open();
               int row = 0;
               int seqno = Convert.ToInt32(ds.Tables[0].Rows[0][0].ToString());
               for (int i = 0; i < ds.Tables[0].Rows.Count; i++)
               {

                   SqlCommand cmd_insert = new SqlCommand("insert into corr_print_maintenance  values ('" + UserLogin.UserName + "','" + estimate + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][0].ToString()) + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][1].ToString()) + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][2].ToString()) + "','" + Convert.ToString(ds.Tables[0].Rows[row][3].ToString()) + "','0','" + do_gsa + "','" + do_mr + "','" + do_speed + "','" + drop_rc + "','" + "No" + "')", conn);
                   cmd_insert.ExecuteNonQuery();
                   row = row + 1;

               }
               conn.Close();
               Response.Write("<script>location.href='corr_print_gsm.aspx?seq=" + seqno + "'</script>");
           }
           else
           {
               Response.Write("<script>self.window.close(); window.opener.location.reload();</script>");
           }
        //}
        //catch { }       
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //Response.Write(HiddenField1.Value);

        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox Qty1 = (TextBox)FormView1.FindControl("TextBox1");
            TextBox Qty2 = (TextBox)FormView1.FindControl("TextBox2");
            TextBox Qty3 = (TextBox)FormView1.FindControl("TextBox3");
            TextBox Qty4 = (TextBox)FormView1.FindControl("TextBox4");
            TextBox Qty5 = (TextBox)FormView1.FindControl("TextBox5");
            TextBox Qty6 = (TextBox)FormView1.FindControl("TextBox6");
            TextBox Qty7 = (TextBox)FormView1.FindControl("TextBox7");
            TextBox Qty8 = (TextBox)FormView1.FindControl("TextBox8");
            TextBox Qty9 = (TextBox)FormView1.FindControl("TextBox9");
            TextBox Qty10 = (TextBox)FormView1.FindControl("TextBox10");
            TextBox Qty11 = (TextBox)FormView1.FindControl("TextBox11");
            TextBox Qty12 = (TextBox)FormView1.FindControl("TextBox12");
            TextBox Qty13 = (TextBox)FormView1.FindControl("TextBox13");
            TextBox Qty14 = (TextBox)FormView1.FindControl("TextBox14");
            TextBox Qty15 = (TextBox)FormView1.FindControl("TextBox15");
            TextBox Qty16 = (TextBox)FormView1.FindControl("TextBox16");
            TextBox Qty17 = (TextBox)FormView1.FindControl("TextBox17");
            TextBox Qty18 = (TextBox)FormView1.FindControl("TextBox18");
            TextBox Qty19 = (TextBox)FormView1.FindControl("TextBox19");
            TextBox Qty20 = (TextBox)FormView1.FindControl("TextBox20");
            TextBox Qty21 = (TextBox)FormView1.FindControl("TextBox21");
            TextBox Qty22 = (TextBox)FormView1.FindControl("TextBox22");
            TextBox Qty23 = (TextBox)FormView1.FindControl("TextBox23");
            TextBox Qty24 = (TextBox)FormView1.FindControl("TextBox24");
            TextBox Qty25 = (TextBox)FormView1.FindControl("TextBox25");
            TextBox Qty26 = (TextBox)FormView1.FindControl("TextBox26");
            TextBox Qty27 = (TextBox)FormView1.FindControl("TextBox27");
            TextBox Qty28 = (TextBox)FormView1.FindControl("TextBox28");

            TextBox Rel1 = (TextBox)FormView1.FindControl("TextBox29");
            TextBox Rel2 = (TextBox)FormView1.FindControl("TextBox30");
            TextBox Rel3 = (TextBox)FormView1.FindControl("TextBox31");
            TextBox Rel4 = (TextBox)FormView1.FindControl("TextBox32");
            TextBox Rel5 = (TextBox)FormView1.FindControl("TextBox33");
            TextBox Rel6 = (TextBox)FormView1.FindControl("TextBox34");
            TextBox Rel7 = (TextBox)FormView1.FindControl("TextBox35");
            TextBox Rel8 = (TextBox)FormView1.FindControl("TextBox36");
            TextBox Rel9 = (TextBox)FormView1.FindControl("TextBox37");
            TextBox Rel10 = (TextBox)FormView1.FindControl("TextBox38");
            TextBox Rel11 = (TextBox)FormView1.FindControl("TextBox39");
            TextBox Rel12 = (TextBox)FormView1.FindControl("TextBox40");
            TextBox Rel13 = (TextBox)FormView1.FindControl("TextBox41");
            TextBox Rel14 = (TextBox)FormView1.FindControl("TextBox42");
            TextBox Rel15 = (TextBox)FormView1.FindControl("TextBox43");
            TextBox Rel16 = (TextBox)FormView1.FindControl("TextBox44");
            TextBox Rel17 = (TextBox)FormView1.FindControl("TextBox45");
            TextBox Rel18 = (TextBox)FormView1.FindControl("TextBox46");
            TextBox Rel19 = (TextBox)FormView1.FindControl("TextBox47");
            TextBox Rel20 = (TextBox)FormView1.FindControl("TextBox48");
            TextBox Rel21 = (TextBox)FormView1.FindControl("TextBox49");
            TextBox Rel22 = (TextBox)FormView1.FindControl("TextBox50");
            TextBox Rel23 = (TextBox)FormView1.FindControl("TextBox51");
            TextBox Rel24 = (TextBox)FormView1.FindControl("TextBox52");
            TextBox Rel25 = (TextBox)FormView1.FindControl("TextBox53");
            TextBox Rel26 = (TextBox)FormView1.FindControl("TextBox54");
            TextBox Rel27 = (TextBox)FormView1.FindControl("TextBox55");
            TextBox Rel28 = (TextBox)FormView1.FindControl("TextBox56");
            Qty1.Focus();
            string estimate = Convert.ToString(Session["order_corrugated_est"]);

            DataSet ds = new DataSet();

            Corrugated corr = new Corrugated();
            ds = corr.SelectDefaultQuantity(UserLogin.UserName, "GetDefaultQty", estimate);


            try
            {                               
                if (Convert.ToString(ds.Tables[0].Rows[0][0]) == "0") Qty1.Text = ""; else Qty1.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                if (Convert.ToString(ds.Tables[0].Rows[0][1]) == "0") Qty2.Text = ""; else Qty2.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                if (Convert.ToString(ds.Tables[0].Rows[0][2]) == "0") Qty3.Text = ""; else Qty3.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                if (Convert.ToString(ds.Tables[0].Rows[0][3]) == "0") Qty4.Text = ""; else Qty4.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
                if (Convert.ToString(ds.Tables[0].Rows[0][4]) == "0") Qty5.Text = ""; else Qty5.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
                if (Convert.ToString(ds.Tables[0].Rows[0][5]) == "0") Qty6.Text = ""; else Qty6.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
                if (Convert.ToString(ds.Tables[0].Rows[0][6]) == "0") Qty7.Text = ""; else Qty7.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
                if (Convert.ToString(ds.Tables[0].Rows[0][7]) == "0") Qty8.Text = ""; else Qty8.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);               
                if (Convert.ToString(ds.Tables[0].Rows[0][8]) == "0") Qty9.Text = ""; else Qty9.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
                if (Convert.ToString(ds.Tables[0].Rows[0][9]) == "0") Qty10.Text = ""; else Qty10.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
                if (Convert.ToString(ds.Tables[0].Rows[0][10]) == "0") Qty11.Text = ""; else Qty11.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
                if (Convert.ToString(ds.Tables[0].Rows[0][11]) == "0") Qty12.Text = ""; else Qty12.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
                if (Convert.ToString(ds.Tables[0].Rows[0][12]) == "0") Qty13.Text = ""; else Qty13.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);
                if (Convert.ToString(ds.Tables[0].Rows[0][13]) == "0") Qty14.Text = ""; else Qty14.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
                if (Convert.ToString(ds.Tables[0].Rows[0][14]) == "0") Qty15.Text = ""; else Qty15.Text = Convert.ToString(ds.Tables[0].Rows[0][14]);
                if (Convert.ToString(ds.Tables[0].Rows[0][15]) == "0") Qty16.Text = ""; else Qty16.Text = Convert.ToString(ds.Tables[0].Rows[0][15]);
                if (Convert.ToString(ds.Tables[0].Rows[0][16]) == "0") Qty17.Text = ""; else Qty17.Text = Convert.ToString(ds.Tables[0].Rows[0][16]);
                if (Convert.ToString(ds.Tables[0].Rows[0][17]) == "0") Qty18.Text = ""; else Qty18.Text = Convert.ToString(ds.Tables[0].Rows[0][17]);
                if (Convert.ToString(ds.Tables[0].Rows[0][18]) == "0") Qty19.Text = ""; else Qty19.Text = Convert.ToString(ds.Tables[0].Rows[0][18]);
                if (Convert.ToString(ds.Tables[0].Rows[0][19]) == "0") Qty20.Text = ""; else Qty20.Text = Convert.ToString(ds.Tables[0].Rows[0][19]);                                            

                
                if (Convert.ToString(ds.Tables[0].Rows[0][28]) == "0") Rel1.Text = ""; else Rel1.Text = Convert.ToString(ds.Tables[0].Rows[0][28]);
                if (Convert.ToString(ds.Tables[0].Rows[0][29]) == "0") Rel2.Text = ""; else Rel2.Text = Convert.ToString(ds.Tables[0].Rows[0][29]);
                if (Convert.ToString(ds.Tables[0].Rows[0][30]) == "0") Rel3.Text = ""; else Rel3.Text = Convert.ToString(ds.Tables[0].Rows[0][30]);
                if (Convert.ToString(ds.Tables[0].Rows[0][31]) == "0") Rel4.Text = ""; else Rel4.Text = Convert.ToString(ds.Tables[0].Rows[0][31]);
                if (Convert.ToString(ds.Tables[0].Rows[0][32]) == "0") Rel5.Text = ""; else Rel5.Text = Convert.ToString(ds.Tables[0].Rows[0][32]);
                if (Convert.ToString(ds.Tables[0].Rows[0][33]) == "0") Rel6.Text = ""; else Rel6.Text = Convert.ToString(ds.Tables[0].Rows[0][33]);
                if (Convert.ToString(ds.Tables[0].Rows[0][34]) == "0") Rel7.Text = ""; else Rel7.Text = Convert.ToString(ds.Tables[0].Rows[0][34]);
                if (Convert.ToString(ds.Tables[0].Rows[0][35]) == "0") Rel8.Text = ""; else Rel8.Text = Convert.ToString(ds.Tables[0].Rows[0][35]);
                if (Convert.ToString(ds.Tables[0].Rows[0][36]) == "0") Rel9.Text = ""; else Rel9.Text = Convert.ToString(ds.Tables[0].Rows[0][36]);
                if (Convert.ToString(ds.Tables[0].Rows[0][37]) == "0") Rel10.Text = ""; else Rel10.Text = Convert.ToString(ds.Tables[0].Rows[0][37]);
                if (Convert.ToString(ds.Tables[0].Rows[0][38]) == "0") Rel11.Text = ""; else Rel11.Text = Convert.ToString(ds.Tables[0].Rows[0][38]);
                if (Convert.ToString(ds.Tables[0].Rows[0][39]) == "0") Rel12.Text = ""; else Rel12.Text = Convert.ToString(ds.Tables[0].Rows[0][39]);
                if (Convert.ToString(ds.Tables[0].Rows[0][40]) == "0") Rel13.Text = ""; else Rel13.Text = Convert.ToString(ds.Tables[0].Rows[0][40]);
                if (Convert.ToString(ds.Tables[0].Rows[0][41]) == "0") Rel14.Text = ""; else Rel14.Text = Convert.ToString(ds.Tables[0].Rows[0][41]);
                if (Convert.ToString(ds.Tables[0].Rows[0][42]) == "0") Rel15.Text = ""; else Rel15.Text = Convert.ToString(ds.Tables[0].Rows[0][42]);
                if (Convert.ToString(ds.Tables[0].Rows[0][43]) == "0") Rel16.Text = ""; else Rel16.Text = Convert.ToString(ds.Tables[0].Rows[0][43]);
                if (Convert.ToString(ds.Tables[0].Rows[0][44]) == "0") Rel17.Text = ""; else Rel17.Text = Convert.ToString(ds.Tables[0].Rows[0][44]);
                if (Convert.ToString(ds.Tables[0].Rows[0][45]) == "0") Rel18.Text = ""; else Rel18.Text = Convert.ToString(ds.Tables[0].Rows[0][45]);
                if (Convert.ToString(ds.Tables[0].Rows[0][46]) == "0") Rel19.Text = ""; else Rel19.Text = Convert.ToString(ds.Tables[0].Rows[0][46]);
                if (Convert.ToString(ds.Tables[0].Rows[0][47]) == "0") Rel20.Text = ""; else Rel20.Text = Convert.ToString(ds.Tables[0].Rows[0][47]);                         
            }
            catch
            {

            }
        }
    }
    protected void GridView1_selectedindex_change(object sender, EventArgs e)
    {
        HiddenField_Vendor.Value = GridView1.SelectedRow.Cells[1].Text;
        Session["estimate_analysis_vendor_no"] = GridView1.SelectedRow.Cells[1].Text;
    }
    protected void ok_button_click(object sender, EventArgs e)
    {
        vendordiv.Visible = false;
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    protected void Price_Button_Click(object sender, EventArgs e)
    {
        HiddenField_Vendor.Value = "bestvendor";
        Session["estimate_analysis_vendor_no"] = "bestvendor";
        vendordiv.Visible = false;
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    protected void Gridview1_Sorting(object sender, GridViewSortEventArgs e)
    {
        
    }
    
}
